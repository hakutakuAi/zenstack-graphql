import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Category, PostCategory, CategoryFilterInput, CategorySortInput, CategoryConnection } from '../../schema'
import type { Context } from './types'
import { buildConnection, buildFilter, buildSort, buildSelect } from '@hakutakuai/zenstack-graphql/helpers'
import { CATEGORY_WITH_POSTS_SELECT, POST_CATEGORY_WITH_RELATIONS_SELECT } from '../select-definitions'

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
		const where = buildFilter(filter as any)
		const orderBy = buildSort(sort as any)
		const select = buildSelect(CATEGORY_WITH_POSTS_SELECT, info)

		const config = buildConnection({
			first,
			after,
			last,
			before,
			where,
			orderBy,
			select,
		})

		const [items, totalCount] = await Promise.all([prisma.category.findMany(config.findMany), prisma.category.count(config.count)])

		return config.toConnection(items, totalCount) as CategoryConnection
	}

	@Query(() => Category, { nullable: true })
	async category(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Category | null> {
		return (await prisma.category.findUnique({ where: { id }, select: CATEGORY_WITH_POSTS_SELECT })) as Category | null
	}

	@Mutation(() => Category)
	async createCategory(
		@Arg('name', () => String) name: string,
		@Arg('description', () => String, { nullable: true }) description: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<Category> {
		return (await prisma.category.create({ data: { name, description }, select: CATEGORY_WITH_POSTS_SELECT })) as Category
	}

	@FieldResolver(() => [PostCategory])
	async posts(@Root() category: Category, @Ctx() { prisma }: Context): Promise<PostCategory[]> {
		return (await prisma.categoryOnPost.findMany({ where: { categoryId: category.id }, select: POST_CATEGORY_WITH_RELATIONS_SELECT })) as PostCategory[]
	}
}
