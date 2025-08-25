import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int } from 'type-graphql'
import { Category, PostCategory, CategoryFilterInput, CategorySortInput, CategoryConnection } from '../../schema'
import { BaseResolver } from './base-resolver'
import type { Context } from './types'

@Resolver(() => Category)
export class CategoryResolver extends BaseResolver {
	private readonly ALLOWED_FILTER_FIELDS = ['name']
	private readonly ALLOWED_SORT_FIELDS: string[] = []

	@Query(() => CategoryConnection)
	async categories(
		@Arg('filter', () => CategoryFilterInput, { nullable: true }) filter: CategoryFilterInput | null,
		@Arg('sort', () => CategorySortInput, { nullable: true }) sort: CategorySortInput | null,
		@Arg('first', () => Int, { nullable: true }) first: number | null,
		@Arg('after', () => String, { nullable: true }) after: string | null,
		@Arg('last', () => Int, { nullable: true }) last: number | null,
		@Arg('before', () => String, { nullable: true }) before: string | null,
		@Ctx() { prisma }: Context,
	): Promise<CategoryConnection> {
		return this.buildRelayConnection<Category>(
			prisma,
			'category',
			{ filter, sort, first, after, last, before },
			this.ALLOWED_FILTER_FIELDS,
			this.ALLOWED_SORT_FIELDS,
		)
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
		return this.findMany<PostCategory>(prisma, 'categoryOnPost', { where: { categoryId: category.id } })
	}
}
