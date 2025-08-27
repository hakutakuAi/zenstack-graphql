import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Category, PostCategory, CategoryFilterInput, CategorySortInput, CategoryConnection, CategoryQueryArgs } from '../../schema'
import { BaseResolver } from './base-resolver'
import type { Context } from './types'
import { ConnectionBuilder, POSTCATEGORY_INCLUDES } from '../../schema-helpers'

@Resolver(() => Category)
export class CategoryResolver extends BaseResolver {
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
		return ConnectionBuilder.buildCategoryConnection(prisma, args, info)
	}

	@Query(() => Category, { nullable: true })
	async category(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Category | null> {
		return this.findUnique<Category>(prisma, 'category', { id })
	}

	@Mutation(() => Category)
	async createCategory(
		@Arg('name', () => String) name: string,
		@Arg('description', () => String, { nullable: true }) description: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<Category> {
		return this.create<Category>(prisma, 'category', { name, description })
	}

	@FieldResolver(() => [PostCategory])
	async posts(@Root() category: Category, @Ctx() { prisma }: Context): Promise<PostCategory[]> {
		return this.findMany<PostCategory>(prisma, 'categoryOnPost', { where: { categoryId: category.id }, include: POSTCATEGORY_INCLUDES })
	}
}
