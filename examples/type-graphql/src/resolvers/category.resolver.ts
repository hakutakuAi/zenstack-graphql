import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Category, PostCategory, CategoryFilterInput, CategorySortInput, CategoryConnection, CategoryQueryArgs } from '../../schema'
import type { Context } from './types'
import { ConnectionBuilder, POSTCATEGORY_INCLUDES } from '../../schema-helpers'

@Resolver(() => Category)
export class CategoryResolver {
	@Query(() => CategoryConnection)
	async categories(
		@Arg('filter', () => CategoryFilterInput, { nullable: true }) filter: CategoryFilterInput | undefined,
		@Arg('sort', () => CategorySortInput, { nullable: true }) sort: CategorySortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<CategoryConnection> {
		const args: CategoryQueryArgs = { filter, sort, first, after, last, before }
		const config = ConnectionBuilder.buildCategoryConnectionConfig(args, info)

		const items = await prisma.category.findMany(config.findManyOptions)
		const totalCount = await prisma.category.count(config.countOptions)

		return ConnectionBuilder.processResults(items, totalCount, config.paginationInfo) as CategoryConnection
	}

	@Query(() => Category, { nullable: true })
	async category(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Category | null> {
		return await prisma.category.findUnique({ where: { id } }) as Category | null
	}

	@Mutation(() => Category)
	async createCategory(
		@Arg('name', () => String) name: string,
		@Arg('description', () => String, { nullable: true }) description: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<Category> {
		return await prisma.category.create({ data: { name, description } }) as Category
	}

	@FieldResolver(() => [PostCategory])
	async posts(@Root() category: Category, @Ctx() { prisma }: Context): Promise<PostCategory[]> {
		return await prisma.categoryOnPost.findMany({ where: { categoryId: category.id }, include: POSTCATEGORY_INCLUDES }) as PostCategory[]
	}
}
